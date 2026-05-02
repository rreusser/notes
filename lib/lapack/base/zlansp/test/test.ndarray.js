/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var format = require( '@stdlib/string/format' );
var zlansp = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureFile = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures', 'zlansp.jsonl' );
var rawLines = readFileSync( fixtureFile, 'utf8' ).trim().split( '\n' );
var FIXTURES = rawLines.map( function parse( line ) {
	return JSON.parse( line );
});


// MATRIX DATA (mirrors test/fortran/test_zlansp.f90) //

// Each entry is a flat real-imag pair sequence representing the packed AP array.

// 3x3 complex symmetric, upper packed:
// (2,1),(1,2),(5,-1),(3,-1),(2,1),(4,2)
var DATA_3x3_U = [
	2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0
];
// 3x3 complex symmetric, lower packed:
// (2,1),(1,2),(3,-1),(5,-1),(2,1),(4,2)
var DATA_3x3_L = [
	2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 5.0, -1.0, 2.0, 1.0, 4.0, 2.0
];

// 4x4 complex symmetric, upper:
// (2,1),(1,2),(5,-1),(3,-1),(2,1),(4,2),(0.5,0.5),(1,-2),(3,0),(6,-3)
var DATA_4x4_U = [
	2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0,
	0.5, 0.5, 1.0, -2.0, 3.0, 0.0, 6.0, -3.0
];
// 4x4 complex symmetric, lower:
// (2,1),(1,2),(3,-1),(0.5,0.5),(5,-1),(2,1),(1,-2),(4,2),(3,0),(6,-3)
var DATA_4x4_L = [
	2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 0.5, 0.5, 5.0, -1.0, 2.0, 1.0,
	1.0, -2.0, 4.0, 2.0, 3.0, 0.0, 6.0, -3.0
];

// 1x1: (3.0, 4.0)
var DATA_1x1 = [ 3.0, 4.0 ];


// FUNCTIONS //

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, format( '%s: expected %s, got %s (rel err %s)', msg, expected, actual, relErr ) );
}

function makeAP( flat ) {
	var buf = new Float64Array( flat );
	return new Complex128Array( buf.buffer );
}

var normMap = {
	'max': 'max',
	'one': 'one-norm',
	'inf': 'inf-norm',
	'frob': 'frobenius'
};
var uploMap = {
	'U': 'upper',
	'L': 'lower'
};

/**
* Parses a zlansp fixture name into routine arguments.
* Patterns:
*   zlansp_3x3_max_U, zlansp_4x4_frob_L
*   zlansp_n0
*   zlansp_1x1_max, zlansp_1x1_one, ...
*
* @private
* @param {string} name - fixture name
* @returns {Object} { N, uplo, norm, data }
*/
function parseName( name ) {
	var parts = name.split( '_' );
	var sizePart = parts[ 1 ];
	var norm;
	var uplo;
	var data;
	var N;

	if ( sizePart === 'n0' ) {
		return { N: 0, uplo: 'upper', norm: 'max', data: [] };
	}

	norm = normMap[ parts[ 2 ] ];

	if ( sizePart === '3x3' ) {
		N = 3;
		uplo = uploMap[ parts[ 3 ] ];
		data = ( uplo === 'upper' ) ? DATA_3x3_U : DATA_3x3_L;
	} else if ( sizePart === '4x4' ) {
		N = 4;
		uplo = uploMap[ parts[ 3 ] ];
		data = ( uplo === 'upper' ) ? DATA_4x4_U : DATA_4x4_L;
	} else if ( sizePart === '1x1' ) {
		N = 1;
		uplo = 'upper';
		data = DATA_1x1;
	} else {
		throw new Error( 'unrecognized fixture: ' + name );
	}

	return { N: N, uplo: uplo, norm: norm, data: data };
}


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zlansp, 'function', 'main export is a function' );
});

FIXTURES.forEach( function build( fx ) {
	test( 'zlansp: ' + fx.name, function t() {
		var args = parseName( fx.name );
		var AP = ( args.N === 0 ) ? new Complex128Array( 0 ) : makeAP( args.data );
		var WORK = new Float64Array( Math.max( args.N, 1 ) );
		var result = zlansp( args.norm, args.uplo, args.N, AP, 1, 0, WORK, 1, 0 );
		assertClose( result, fx.result, 1e-12, fx.name );
	});
});
