{
  description = "FIXME: Your package description";

  nixConfig.extra-substituters = "https://emacs-ci.cachix.org";
  nixConfig.extra-trusted-public-keys = "emacs-ci.cachix.org-1:B5FVOrxhXXrOL0S+tQ7USrhjMT5iOPH+QN9q0NItom4=";

  inputs = {
    gnu-elpa = {
      url = "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
      flake = false;
    };
    melpa = {
      url = "github:akirak/melpa/org-memento";
      flake = false;
    };
    epkgs = {
      url = "github:emacsmirror/epkgs";
      flake = false;
    };
    emacs = {
      url = "github:emacs-mirror/emacs";
      flake = false;
    };

    nomake = {
      url = "github:emacs-twist/nomake";
      inputs.gnu-elpa.follows = "gnu-elpa";
      inputs.melpa.follows = "melpa";
      inputs.epkgs.follows = "epkgs";
      inputs.emacs.follows = "emacs";
    };
  };

  outputs = {
    self,
    nomake,
    ...
  } @ inputs:
    nomake.lib.mkFlake {
      src = ./.;
      localPackages = [
        "org-memento"
      ];

      extraPackages = [
        "org"
      ];

      scripts = {
        test = {
          description = "Run buttercup tests";
          compile = false;
          extraPackages = [
            "buttercup"
          ];
          text = ''
            emacs -batch -L . -l buttercup -f buttercup-run-discover
          '';
        };

        test-export = {
          compile = false;
          text = ''
            emacs -batch -L . --eval "(setq org-memento-file \"test/memento3.org\")" \
              -l org-memento-export \
              --eval "(setq make-backup-files nil)" \
              --eval "(org-memento-export-to-json \"2021-12-31\" \"2021-12-31\" \"test/export.json\")"

            # You need to have a node executable in the testing environment.
            # Due to a limitation with this framework, it cannot be provided
            # from Nix, but it runs on GitHub Actions.

            cd node
            node index.js validate -f ../test/export.json
          '';
        };
      };
    };
}
