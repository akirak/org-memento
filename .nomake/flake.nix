{
  description =
    "THIS IS AN AUTO-GENERATED FILE. PLEASE DON'T EDIT IT MANUALLY.";
  inputs = {
    buttercup = {
      flake = false;
      owner = "jorgenschaefer";
      repo = "emacs-buttercup";
      type = "github";
    };
    compat = {
      flake = false;
      type = "git";
      url = "https://git.sr.ht/~pkal/compat";
    };
    dash = {
      flake = false;
      owner = "magnars";
      repo = "dash.el";
      type = "github";
    };
    magit-section = {
      flake = false;
      owner = "magit";
      repo = "magit";
      type = "github";
    };
    org = {
      flake = false;
      ref = "bugfix";
      type = "git";
      url = "git://git.sv.gnu.org/emacs/org-mode.git";
    };
    org-memento = {
      flake = false;
      owner = "akirak";
      repo = "org-memento";
      type = "github";
    };
    taxy = {
      flake = false;
      owner = "alphapapa";
      repo = "taxy.el";
      type = "github";
    };
  };
  outputs = { ... }: { };
}
