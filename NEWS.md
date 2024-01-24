# charpente 0.7.0

## New:
- Enhanced `esbuild` configuration to allow component based bundling. `build_js` now accepts an `entry_points` argument to specify the entry files processed by `esbuild`. The (minified) output files will correspond to the given entry files. The default is monolithic building with the `./srcjs/main.js` file. See `?build_js` for more details.

# charpente 0.6.0

## New:
- Allow to initialise `esbuild` and `mocha` in a standalone way, without the need of the full `charpente` workflow. This is useful if you want to use `esbuild` and its plugins in already existing projects. See `?set_esbuild` and `?set_mocha` for more details.

## Improvements:
- Move `use_build_ignore()` and `use_git_ignore()` calls to `set_esbuild()` to enable standalone usage.

# charpente 0.5.0

## Breaking change:
- Include Sass handling. SCSS files are sorted in the `/styles`folder and esbuild
has new Sass modules to treat them and generate CSS. 
`package.json` now calls `node esbuild.dev.js` or `node esbuild.prod.js`,
depending on the selected mode, that is production or development. 
`esbuild.**.js` is a new script which will be processed at run time in your project, before being called by `node` in `package.json`. If you come from an older
`{charpente}` version, `build_js()` will first try to create a `/styles` folder (which you normally don't have) and install missing Sass dependencies next to esbuild (`esbuild-sass-plugin`, `postcss`, `autoprefixer`). 
- `get_dependency_assets()` leverages the new jsdlivr algorithm to infer the best entry point scripts for JS and CSS files, when downloading dependencies within `create_dependency()`. 
This likely will change your vendors dependencies scripts names but should not change the features. 
- `charpente_options()` will likely be deprecated because of the previous point. It still
contains the local option to either point to external CDN or copy external vendore files
into the local project.
- `create_css()` has been replaced by `create_scss()` (you can still use `golem::create_css`).
There is one main SCSS file created at project setup. Other SCSS files are referenced into this main `styles/main.scss` using `@import path`, which will allow you to have modular
Sass code.
- Remove `entry_point` param from `build_js()` as it was not used anyway...

# charpente 0.1.0

* Added a `NEWS.md` file to track changes to the package.
