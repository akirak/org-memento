{
  description = "FIXME: Your package description";

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

  outputs =
    { self
    , nomake
    , ...
    } @ inputs:
    nomake.lib.mkFlake {
      src = ./.;
      localPackages = [
        "org-memento"
      ];

      scripts = {
        test = {
          description = "Run buttercup tests";
          compile = true;
          extraPackages = [
            "buttercup"
          ];
          text = ''
            emacs -batch -l buttercup -f buttercup-run-discover "$PWD"
          '';
        };
      };
    };
}
