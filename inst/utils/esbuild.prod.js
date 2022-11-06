import esbuild from "esbuild";
import {sassPlugin} from 'esbuild-sass-plugin';
import postcss from 'postcss';
import autoprefixer from 'autoprefixer';

esbuild
  .build({
    entryPoints: ["styles/main.scss", "srcjs/main.js"],
    outdir: "inst/<<name>>-<<version>>",
    bundle: true,
    format: "ems",
    minify: true, // prod
    sourcemap: true, // prod
    plugins: [
      sassPlugin({
        async transform(source) {
          const { css } = await postcss([autoprefixer]).process(source);
          return css;
        },
      }),
    ]
  })
  .then(() => console.log("⚡ Build complete! ⚡"))
  .catch(() => process.exit(1));

esbuild srcjs/<<entry_point>> --bundle --minify --sourcemap --format=esm --outfile=inst/<<name>>-<<version>>/js/<<name>>.min.js
