var Complex128Array = require('@stdlib/array/complex128');
var reinterpret = require('@stdlib/strided/base/reinterpret-complex128');
var zlaqr5 = require('/Users/rreusser/gh/rreusser/notes/blahpack/lib/lapack/base/zlaqr5/lib/base.js');
var readFileSync = require('fs').readFileSync;
var path = require('path');

var fixtureDir = path.join('/Users/rreusser/gh/rreusser/notes/blahpack', 'test', 'fixtures');
var lines = readFileSync(path.join(fixtureDir, 'zlaqr5.jsonl'), 'utf8').trim().split('\n');
var fixture = lines.map(function(line) { return JSON.parse(line); });
var tc = fixture.find(function(t) { return t.name === '6x6_2shifts'; });

var n = 6;
var nshfts = 2;

function makeMatrix(N) {
    return { data: new Complex128Array(N*N), s1: 1, s2: N, offset: 0 };
}
function mset(m, N, i, j, re, im) {
    var mv = reinterpret(m.data, 0);
    var idx = (m.offset + i * m.s1 + j * m.s2) * 2;
    mv[idx] = re; mv[idx+1] = im;
}

var Hm = makeMatrix(n);
var Zm = makeMatrix(n);
var Vm = makeMatrix(3);
var S = new Complex128Array(nshfts);

mset(Hm,n,0,0,5.0,1.0); mset(Hm,n,0,1,2.0,-0.5); mset(Hm,n,0,2,1.0,0.0);
mset(Hm,n,0,3,0.5,0.2); mset(Hm,n,0,4,0.25,0.0); mset(Hm,n,0,5,0.1,-0.1);
mset(Hm,n,1,0,1.0,0.0); mset(Hm,n,1,1,4.0,-1.0); mset(Hm,n,1,2,1.5,0.5);
mset(Hm,n,1,3,0.5,0.0); mset(Hm,n,1,4,0.3,0.1); mset(Hm,n,1,5,0.15,0.0);
mset(Hm,n,2,1,0.8,0.2); mset(Hm,n,2,2,3.0,0.5); mset(Hm,n,2,3,1.0,-0.5);
mset(Hm,n,2,4,0.5,0.0); mset(Hm,n,2,5,0.25,0.1);
mset(Hm,n,3,2,0.6,-0.1); mset(Hm,n,3,3,2.5,-0.5); mset(Hm,n,3,4,1.0,0.5);
mset(Hm,n,3,5,0.5,0.0);
mset(Hm,n,4,3,0.4,0.15); mset(Hm,n,4,4,2.0,0.0); mset(Hm,n,4,5,1.0,-0.5);
mset(Hm,n,5,4,0.3,-0.1); mset(Hm,n,5,5,1.5,1.0);

for (var i = 0; i < n; i++) mset(Zm,n,i,i,1.0,0.0);

var Sv = reinterpret(S, 0);
Sv[0]=3.0; Sv[1]=1.0; Sv[2]=2.0; Sv[3]=-0.5;

var nu = 2*nshfts+1;
var Um = { data: new Complex128Array(nu*nu), s1:1, s2:nu, offset:0 };
var WVm = { data: new Complex128Array(n*nu), s1:1, s2:n, offset:0 };
var WHm = { data: new Complex128Array(nu*n), s1:1, s2:nu, offset:0 };

zlaqr5(true, true, 0, n, 1, n, nshfts,
    S, 1, 0,
    Hm.data, Hm.s1, Hm.s2, Hm.offset,
    1, n,
    Zm.data, Zm.s1, Zm.s2, Zm.offset,
    Vm.data, Vm.s1, Vm.s2, Vm.offset,
    Um.data, Um.s1, Um.s2, Um.offset,
    n, WVm.data, WVm.s1, WVm.s2, WVm.offset,
    n, WHm.data, WHm.s1, WHm.s2, WHm.offset);

var Hv = reinterpret(Hm.data, 0);
var expected = tc.H;

// Print comparison
console.log("H result vs expected:");
for (var i = 0; i < expected.length; i++) {
    var diff = Math.abs(Hv[i] - expected[i]);
    if (diff > 1e-12) {
        var r = Math.floor(i/2) % n;
        var c = Math.floor(Math.floor(i/2) / n);  // column-major? Let me check
        console.log("  H[" + i + "] (flat): got=" + Hv[i].toFixed(8) + " exp=" + expected[i].toFixed(8) + " diff=" + diff.toExponential(4));
    }
}
