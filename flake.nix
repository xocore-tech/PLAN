{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
  };

  outputs = { self, nixpkgs }: {

    devShell.x86_64-linux =
      with nixpkgs.legacyPackages.x86_64-linux;
      let
        pkgs = import nixpkgs { system = "x86_64-linux"; };

        tex = pkgs.texlive.combine {
          inherit (pkgs.texlive)
            scheme-small   # minimal base LaTeX
            needspace
            tcolorbox
            latexmk;       # build tool
        };

        ghcEnv = pkgs.haskellPackages.ghcWithPackages
                   (hp: with hp; [
                     text primitive pretty-show containers deepseq
                     optics ghc-prim mtl transformers vector
                   ]);

        prettyprinter = pkgs.python3Packages.buildPythonPackage rec {
          pname = "prettyprinter";
          version = "0.18.0"; # or whatever version you need
          src = pkgs.fetchPypi {
            inherit pname version;
            sha256 = "sha256-n+XafsU1EIgd0116XGd7pF80z+ao540avSBlLPghOag=";
          };

          propagatedBuildInputs = [
            pkgs.python3Packages.colorful
            pkgs.python3Packages.pygments
          ];

          nativeBuildInputs = [
              pkgs.python3Packages.pytest
          ];

          postPatch = ''
            sed -i '/pytest-runner/d' setup.py
          '';

          doCheck = false;
        };

        pyEnv = pkgs.python3.withPackages (pp: [
          pp.tqdm
          prettyprinter
        ]);

      in
        pkgs.mkShell {
          hardeningDisable = [ "fortify" ];
          buildInputs = [
            ghcEnv
            tex
            pyEnv
            pkgs.binutils
            pkgs.gnumake
            pkgs.gnumake.info
            pkgs.binutils.info
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.stylish-haskell
            pkgs.texinfoInteractive
            pkgs.pinfo
            pkgs.rr
            # pkgs.gdb.override { python = pyEnv; }
          ];

          shellHook = ''
            export INFOPATH="${pkgs.binutils.info}/share/info:${pkgs.gnumake.info}/share/info:$$INFOPATH"
          '';
        };
  };
}
