import esbuild from "esbuild";
import {sassPlugin} from 'esbuild-sass-plugin';
import postcss from 'postcss';
import autoprefixer from 'autoprefixer';

esbuild
  .build({
    entryPoints: [<<entry_point>>],
    outdir: "inst/<<name>>-<<version>>/dist",
    entryNames: "<<entry_name>>.min",
    bundle: true,
    format: "esm",
    minify: false, // dev
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
