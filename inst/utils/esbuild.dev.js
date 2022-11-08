import esbuild from "esbuild";
import {sassPlugin} from 'esbuild-sass-plugin';
import postcss from 'postcss';
import autoprefixer from 'autoprefixer';

esbuild
  .build({
    entryPoints: ["./srcjs/main.js"],
    outfile: "inst/<<name>>-<<version>>/dist/<<name>>.js",
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
