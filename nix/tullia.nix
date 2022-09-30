self: system:

let
  inherit (self.inputs.nixpkgs) lib;

  ciInputName = "GitHub event";
in rec {
  tasks = let
    inherit (self.inputs.tullia) flakeOutputTasks taskSequence;

    common = {
      config,
      ...
    }: {
      preset = {
        # needed on top-level task to set runtime options
        nix.enable = true;

        github-ci = {
          enable = config.actionRun.facts != {};
          repo = "input-output-hk/cardano-node";
          sha = config.preset.github-ci.lib.getRevision ciInputName null;
          clone = false;
        };
      };
    };

    mkHydraJobTask = flakeOutputTask: {config, lib, ...}: {
      imports = [common flakeOutputTask];

      flakeOutputTask.flakeUrl = lib.mkIf (config.actionRun.facts != {}) (
        with config.preset.github-ci;
        "github:${repo}/${sha}"
      );

      # some hydra jobs run NixOS tests
      env.NIX_CONFIG = ''
        extra-system-features = kvm
      '';

      memory = 1024 * 8;
      nomad.resources.cpu = 10000;
    };
    mkHydraJobTasks = __mapAttrs (_: mkHydraJobTask);

    # the attribute name in `hydraJobs` for the current system
    os = {
      x86_64-linux = "linux";
      x86_64-darwin = "macos";
    }.${system};

    hydraJobTasks   = mkHydraJobTasks (flakeOutputTasks ["hydraJobs"   os] self);
    hydraJobPrTasks = mkHydraJobTasks (flakeOutputTasks ["hydraJobsPr" os] self);

    ciPushTasks = taskSequence "ci/push/" hydraJobTasks   (__attrNames hydraJobTasks);
    ciPrTasks   = taskSequence "ci/pr/"   hydraJobPrTasks (__attrNames hydraJobPrTasks);
  in
    ciPushTasks // ciPrTasks // {
      "ci/push" = {lib, ...}: {
        imports = [common];
        after = [(lib.last (__attrNames ciPushTasks))];
      };

      "ci/pr" = {lib, ...}: {
        imports = [common];
        after = [(lib.last (__attrNames ciPrTasks))];
      };
    };

  actions = {
    "cardano-node/ci/push" = {
      task = "ci/push";
      io = ''
        #lib.io.github_push
        #input: "${ciInputName}"
        #repo: "input-output-hk/cardano-node"
        #default_branch: false
      '';
    };

    "cardano-node/ci/pr" = {
      task = "ci/pr";
      io = ''
        #lib.io.github_pr
        #input: "${ciInputName}"
        #repo: "input-output-hk/cardano-node"
        #target_default: false
      '';
    };
  };
}
