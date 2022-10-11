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
    org-memento = {
      flake = false;
      owner = "~akirak";
      repo = "org-memento";
      type = "sourcehut";
    };
    org-memento-timeline = {
      flake = false;
      owner = "~akirak";
      repo = "org-memento";
      type = "sourcehut";
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
