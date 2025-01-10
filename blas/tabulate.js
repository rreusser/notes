import fs from 'fs';
import path from 'path';
import { fetchDirectoryContents } from './gh.js';

function createPatternTester (patternList) {
  return function (str) {
    for (const pattern of patternList) {
      if (typeof pattern === 'string') {
        if (pattern === str) return true;
      } else {
        if (pattern.test(str)) return true;
      }
    }
  }
  return false;
}

function parseBLAS (str) {
  let prefix = '';
  if (str[0] === 'I') {
    prefix = 'I';
    str = str.substr(1);
  }
  const type = str[0];
  const rest = str.substr(1);
  return [prefix + type, rest];
}

function blasComparator ({name: str1}, {name: str2}) {
  const [type1, func1] = parseBLAS(str1);
  const [type2, func2] = parseBLAS(str2);
  if (func1 === func2) return type1.localeCompare(type2);
  return func1.localeCompare(func2);
}

const { GITHUB_PAT } = JSON.parse(fs.readFileSync(path.join(import.meta.dirname, 'credentials.json'), 'utf8'));
const BLAS = JSON.parse(fs.readFileSync(path.join(import.meta.dirname, 'blas.json'), 'utf8'));
const LEVEL1 = BLAS.filter(({level}) => level === 1);
const LEVEL2 = BLAS.filter(({level}) => level === 2);
const LEVEL3 = BLAS.filter(({level}) => level === 3);

LEVEL1.sort(blasComparator);
LEVEL2.sort(blasComparator);
LEVEL3.sort(blasComparator);

//for (const {name} of LEVEL1) console.log(name);

const owner = "stdlib-js";
const repo = "stdlib";
const directoryPath = "lib/node_modules/@stdlib/blas/base";

const contents = await fetchDirectoryContents(owner, repo, directoryPath, GITHUB_PAT);

const exclude = createPatternTester([
  'assert',
  'dir',
  'docs',
  'examples',
  'lib',
  'shared',
  'test',
  /^diagonal-type/,
  /^transpose-operation/,
  /^matrix-triangle/,
  /^layout/,
  /^operation-side/
]);

function makeTable (funcs) {
  const lines = [];
  lines.push('| Netlib | Stdlib | WASM | Description |');
  lines.push('| ------ | ------ | ---- | ----------- |');

  for (const func of funcs) {
    const slug = func.name.toLowerCase();
    const found = contents.find(({name}) => name === slug);
    const foundWasm = contents.find(({name}) => name === `${slug}-wasm`);

    let stdlib = '';
    if (found) {
      stdlib = `[${found.name}](${found.html_url})`;
    }
    let wasm = '';
    if (foundWasm) {
      wasm = `[wasm](${foundWasm.html_url})`;
    }
    
    lines.push(`| [${func.name}](${func.url}) | ${stdlib} | ${wasm} | ${func.description} |`);
  }
  lines.push('\n');

  return lines.join('\n');
}


const md = [];
md.push('# BLAS');

md.push(`Prefixes:
- \`S\`: single precision
- \`D\`: double precision
- \`C\`: single precision complex
- \`Z\`: double precision complex
`);


md.push('## Level 1');
md.push(makeTable(LEVEL1));

md.push('## Level 2');
md.push(makeTable(LEVEL2));

md.push('## Level 3');
md.push(makeTable(LEVEL3));

fs.writeFileSync('README.md', md.join('\n'));
