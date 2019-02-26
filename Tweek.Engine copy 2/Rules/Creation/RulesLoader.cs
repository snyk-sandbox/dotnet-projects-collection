﻿using LanguageExt;
using Soluto.Collections;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Tweek.Engine.Core;
using Tweek.Engine.Core.Rules;
using Tweek.Engine.Core.Utils;
using Tweek.Engine.DataTypes;
using Tweek.Engine.Drivers.Rules;

namespace Tweek.Engine.Rules.Creation
{
    public delegate IRuleParser GetRuleParser(string format);

    public static class RulesLoader
    {
        public static async Task<Func<(GetRule, PathExpander)>> Factory(IRulesRepository repository, GetRuleParser parserResolver)
        {
            (GetRule, PathExpander)? instance = null;
            repository.OnRulesChange += (newRules) => instance = Parse(newRules, parserResolver);
            var initRepo = Parse(await repository.GetAllRules(), parserResolver);
            instance = instance ?? initRepo;
            return () => instance.Value;
        }

        public static (GetRule, PathExpander) Parse(IDictionary<string, RuleDefinition> rules, GetRuleParser parserResolver)
        {
            var tree = new RadixTree<IRule>(rules.ToDictionary(x => x.Key.ToLower(), x => parserResolver(x.Value.Format).Parse(x.Value.Payload)));

            Option<IRule> RulesRepository(ConfigurationPath path) => tree.TryGetValue(path, out var rule) ? Option<IRule>.Some(rule) : Option<IRule>.None;

            IEnumerable<ConfigurationPath> PathExpander(ConfigurationPath path)
            {
                if (!path.IsScan) return new[] { path };

                var keys = path == ConfigurationPath.FullScan
                    ? tree.Keys
                    : tree.ListPrefix($"{path.Folder}/").Select(c => c.key);

                return keys.Select(ConfigurationPath.New).Where(x => !x.IsHidden(path.Folder));
            }

            return (RulesRepository, PathExpander);
        }
    }
}
