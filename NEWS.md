# charpent 0.2.0.9000

## Breaking change:
- Include Sass handling. SCSS files are sorted in the `/styles`folder and esbuild
has new Sass modules. `package.json` now calls `node esbuild.dev.js` or `node esbuild.prod.js`,
depending on the selected production mode. `esbuild.**.js` is a new script which will 
be processed at run time in your project, before calling `node`. If you come from an older
`{charpente}` version, `build_js()` will first try to create a `/styles` folder (which you normally don't have) and install missing Sass dependencies next to esbuild. 
- Use new jsdlivr algorithm to infer entrypoint scripts for JS and CSS files, when
downloading dependencies. This likely will change your vendors dependencies scripts
names but should not change the features. 
- `charpente_options()` will likely be deprecated because of the previous point. 
- `create_css()` has been replaced by `create_scss()`. SCSS files are referenced into
the main `styles/main.scss` using `@import path`, which will allow you to have modular
Sass code.
- Remove `entry_point` param from `build_js()` as it was not used anyway...

# charpente 0.1.0

* Added a `NEWS.md` file to track changes to the package.
