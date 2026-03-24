'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlarfx = require( './../lib/base.js' );

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

test( 'dlarfx: left side M=2, N=3', function t() {
	var v = new Float64Array([ 1.0, 0.5 ]);
	var tau = 1.6;
	var C = new Float64Array([ 1, 2, 3, 4, 5, 6 ]);
	var WORK = new Float64Array( 3 );

	dlarfx( 'L', 2, 3, v, 1, 0, tau, C, 1, 2, 0, WORK, 1, 0 );

	assertClose( C[ 0 ], -2.2, 1e-12, 'C(1,1)' );
	assertClose( C[ 1 ], 0.4, 1e-12, 'C(2,1)' );
});

test( 'dlarfx: right side M=3, N=2', function t() {
	var v = new Float64Array([ 1.0, 0.5 ]);
	var tau = 1.6;
	var C = new Float64Array([ 1, 2, 3, 4, 5, 6 ]);
	var WORK = new Float64Array( 3 );

	dlarfx( 'R', 3, 2, v, 1, 0, tau, C, 1, 3, 0, WORK, 1, 0 );

	assertClose( C[ 0 ], -3.8, 1e-12, 'C(1,1)' );
	assertClose( C[ 3 ], 1.6, 1e-12, 'C(1,2)' );
});

test( 'dlarfx: tau=0 does nothing', function t() {
	var v = new Float64Array([ 1.0, 0.5 ]);
	var C = new Float64Array([ 1, 2, 3, 4 ]);
	var WORK = new Float64Array( 2 );
	var Ccopy = Float64Array.from( C );

	dlarfx( 'L', 2, 2, v, 1, 0, 0.0, C, 1, 2, 0, WORK, 1, 0 );

	for ( var i = 0; i < 4; i++ ) {
		assert.strictEqual( C[ i ], Ccopy[ i ], 'C[' + i + '] unchanged' );
	}
});
