// Shim for Node.js 'module' builtin — createRequire is not needed
// because esbuild resolves all require() calls at bundle time.
export function createRequire() {
  return function require(id) {
    throw new Error(`Cannot require('${id}') in the browser`);
  };
}
