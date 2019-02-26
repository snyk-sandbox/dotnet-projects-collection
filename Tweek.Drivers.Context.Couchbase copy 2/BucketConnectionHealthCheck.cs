﻿using Couchbase.Core;
using System;
using System.Threading;
using System.Threading.Tasks;
using App.Metrics.Health;
using Microsoft.Extensions.Logging;

namespace Tweek.Drivers.Context.Couchbase
{
    internal class BucketConnectionHealthCheck : HealthCheck
    {
        private DateTime _lastSuccessCheck;

        private readonly string _bucketName;
        private readonly Func<string, IBucket> _getBucket;

        private readonly ILogger _logger;
        
        private readonly TimeSpan _timeout;
        private readonly int _retryCount;
        private int failedRetries = 0;

        private HealthCheckResult _state = HealthCheckResult.Unhealthy();

        internal BucketConnectionHealthCheck(Func<string, IBucket> getBucket, string bucketNameToCheck, TimeSpan timeout, int retryCount, ILogger logger)
            : base("CouchbaseConnection")
        {
            _getBucket = getBucket;
            _bucketName = bucketNameToCheck;
            _timeout = timeout;
            _retryCount = retryCount;
            _logger = logger;
        }

        protected override async ValueTask<HealthCheckResult> CheckAsync(
            CancellationToken cancellationToken = new CancellationToken())
        {
            if (DateTime.UtcNow - _lastSuccessCheck > TimeSpan.FromSeconds(1))
            {
                try
                {
                    var bucket = _getBucket(_bucketName);
                    await UpsertHealthcheckKey(bucket);
                    _lastSuccessCheck = DateTime.UtcNow;
                    if ( _state.Status == HealthCheckStatus.Unhealthy){
                        _logger.LogInformation("Couchbase connection is healthy again");
                    }
                    failedRetries = 0;
                    _state = HealthCheckResult.Healthy();
                }
                catch
                {
                    _logger.LogWarning("Couchbase Healthcheck has failed for {RetryCount}", failedRetries);
                    if (++failedRetries > _retryCount){
                        _state =  HealthCheckResult.Unhealthy($"Unavailable since {_lastSuccessCheck}");
                    }
                }
            }
            return _state;
        }

        private async Task UpsertHealthcheckKey(IBucket bucket)
        {
            var upsertTask = bucket.UpsertAsync("healthcheck", "test");
            if (await Task.WhenAny(upsertTask, Task.Delay(_timeout)) != upsertTask)
            {
                throw new Exception("Timeout upserting healthcheck key");
            }
            var result = await upsertTask;
            if (!result.Success){
                throw result.Exception ?? new Exception("Failed to upsert healthcheck key");
            }
        }
    }
}