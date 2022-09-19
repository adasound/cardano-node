let
  ciInputName = "GitHub event";
in {
  tasks = let
    common = {
      config,
      ...
    }: {
      preset = {
        nix.enable = true;
        github-ci = {
          enable = config.actionRun.facts != {};
          repo = "input-output-hk/cardano-node";
          sha = config.preset.github-ci.lib.getRevision ciInputName null;
          clone = false;
        };
      };
    };

    flakeUrl = {
      config,
      lib,
      ...
    }: lib.escapeShellArg (
      if config.actionRun.facts != {}
      then with config.preset.github-ci; "github:${repo}/${sha}"
      else "."
    );
  in {
    "ci/push" = args: {
      imports = [common];

      command.text = ''
        nix build -L ${flakeUrl args}#hydraJobs.required
      '';

      memory = 1024 * 8;
      nomad.resources.cpu = 3500;
    };

    "ci/pr" = args: {
      imports = [common];

      command.text = ''
        nix build -L ${flakeUrl args}#hydraJobsPr.required
      '';

      memory = 1024 * 8;
      nomad.resources.cpu = 3500;
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
