import esbuild from "esbuild";
import {sassPlugin} from 'esbuild-sass-plugin';
import postcss from 'postcss';
import autoprefixer from 'autoprefixer';

esbuild
  .build({
    entryPoints: [<<entry_point>>],
    outfile: "inst/<<name>>-<<version>>/dist/<<name>>.min.js",
    bundle: true,
    format: "esm",
    minify: true, // prod
    sourcemap: "external", // prod
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
