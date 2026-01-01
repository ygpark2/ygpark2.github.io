const fs = require('fs');
const path = require('path');
const sass = require('sass');

const root = path.resolve(__dirname, '..');
const nm = path.join(root, 'node_modules');

function ensureDir(dirPath) {
  fs.mkdirSync(dirPath, { recursive: true });
}

function copyFile(src, dst) {
  ensureDir(path.dirname(dst));
  fs.copyFileSync(src, dst);
}

function copyDir(src, dst) {
  if (!fs.existsSync(src)) {
    throw new Error(`Missing source directory: ${src}`);
  }
  fs.rmSync(dst, { recursive: true, force: true });
  ensureDir(dst);
  for (const entry of fs.readdirSync(src, { withFileTypes: true })) {
    const srcPath = path.join(src, entry.name);
    const dstPath = path.join(dst, entry.name);
    if (entry.isDirectory()) {
      copyDir(srcPath, dstPath);
    } else if (entry.isFile()) {
      copyFile(srcPath, dstPath);
    }
  }
}

function mustExist(targetPath) {
  if (!fs.existsSync(targetPath)) {
    throw new Error(`Missing file: ${targetPath}`);
  }
}

const files = [
  {
    src: path.join(nm, 'd3', 'dist', 'd3.js'),
    dst: path.join(root, 'assets', 'js', 'd3', 'd3.js'),
  },
  {
    src: path.join(nm, 'd3', 'dist', 'd3.min.js'),
    dst: path.join(root, 'assets', 'js', 'd3', 'd3.min.js'),
  },
  {
    src: path.join(nm, 'minimal-mistakes', 'assets', 'js', 'main.min.js'),
    dst: path.join(root, 'assets', 'js', 'minimal-mistakes.js'),
  },
  {
    src: path.join(nm, '@material-tailwind', 'html', 'styles', 'material-tailwind.css'),
    dst: path.join(root, 'assets', 'css', 'material-tailwind.css'),
  },
  {
    src: path.join(nm, 'prismjs', 'prism.js'),
    dst: path.join(root, 'assets', 'js', 'prism', 'prism.js'),
  },
  {
    src: path.join(nm, 'prismjs', 'plugins', 'autoloader', 'prism-autoloader.min.js'),
    dst: path.join(root, 'assets', 'js', 'prism', 'prism-autoloader.min.js'),
  },
  {
    src: path.join(nm, 'prismjs', 'themes', 'prism.css'),
    dst: path.join(root, 'assets', 'css', 'prism.css'),
  },
];

for (const file of files) {
  mustExist(file.src);
  copyFile(file.src, file.dst);
}

copyDir(
  path.join(nm, 'prismjs', 'components'),
  path.join(root, 'assets', 'js', 'prism', 'components')
);

const mmScssSrc = path.join(nm, 'minimal-mistakes', 'assets', 'css', 'main.scss');
const mmCssDst = path.join(root, 'assets', 'css', 'minimal-mistakes.css');
mustExist(mmScssSrc);

const scssRaw = fs.readFileSync(mmScssSrc, 'utf-8');
const scss = scssRaw
  .replace(/^---[\s\S]*?---\s*/m, '')
  .replace(
    '@import \"minimal-mistakes/skins/{{ site.minimal_mistakes_skin | default: \'default\' }}\"; // skin',
    '@import \"minimal-mistakes/skins/default\"; // skin'
  );

const mmSassResult = sass.compileString(scss, {
  loadPaths: [path.join(nm, 'minimal-mistakes', '_sass')],
  syntax: 'scss',
  quietDeps: true,
  logger: {
    warn: () => {},
  },
  style: 'compressed',
});

ensureDir(path.dirname(mmCssDst));
fs.writeFileSync(mmCssDst, mmSassResult.css);

console.log('JS libraries updated in assets/js.');
