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
var zlanhf = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureFile = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures', 'zlanhf.jsonl' );
var rawLines = readFileSync( fixtureFile, 'utf8' ).trim().split( '\n' );
var FIXTURES = rawLines.map( function parse( line ) {
	return JSON.parse( line );
});


// FUNCTIONS //

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, format( '%s: expected %s, got %s (rel err %s)', msg, expected, actual, relErr ) );
}

var normMap = {
	'max': 'max',
	'one': 'one-norm',
	'inf': 'inf-norm',
	'frob': 'frobenius'
};
var transrMap = {
	'N': 'no-transpose',
	'C': 'conjugate-transpose'
};
var uploMap = {
	'U': 'upper',
	'L': 'lower'
};

/**
* Parses a zlanhf fixture name into routine arguments.
* Patterns:
*   zlanhf_n0
*   zlanhf_n1
*   zlanhf_<N>_<transr><uplo>_<norm> e.g. zlanhf_5_NU_max
*
* @private
* @param {string} name - fixture name
* @returns {Object} { N, transr, uplo, norm, key }
*/
function parseName( name ) {
	var parts = name.split( '_' ); // ['zlanhf', ...]
	var rest = parts.slice( 1 );
	var N;
	var transr;
	var uplo;
	var norm;
	var key;

	if ( rest[ 0 ] === 'n0' ) {
		return {
			N: 0,
			transr: 'no-transpose',
			uplo: 'upper',
			norm: 'max',
			key: 'n0'
		};
	}
	if ( rest[ 0 ] === 'n1' ) {
		return {
			N: 1,
			transr: 'no-transpose',
			uplo: 'upper',
			norm: 'max',
			key: 'n1'
		};
	}

	// rest = [<N>, <TU>, <norm>]
	N = parseInt( rest[ 0 ], 10 );
	transr = transrMap[ rest[ 1 ].charAt( 0 ) ];
	uplo = uploMap[ rest[ 1 ].charAt( 1 ) ];
	norm = normMap[ rest[ 2 ] ];
	key = rest[ 0 ] + '_' + rest[ 1 ];

	return { N: N, transr: transr, uplo: uplo, norm: norm, key: key };
}


// Carry rfp data across fixtures sharing the same key prefix.
// As we iterate through FIXTURES in order, when a fixture has `rfp`,
// we cache it under its key; subsequent fixtures with the same key
// reuse the cached rfp.
var RFP_BY_KEY = {};

FIXTURES.forEach( function buildEntry( fx ) {
	var args = parseName( fx.name );
	if ( fx.rfp ) {
		RFP_BY_KEY[ args.key ] = fx.rfp;
	}
	fx._args = args;
	fx._rfp = fx.rfp || RFP_BY_KEY[ args.key ];
});


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zlanhf, 'function', 'main export is a function' );
});

FIXTURES.forEach( function build( fx ) {
	test( 'zlanhf: ' + fx.name, function t() {
		var args = fx._args;
		var N = args.N;
		var rfp = fx._rfp;
		var A;
		var WORK;
		var result;

		if ( N === 0 ) {
			A = new Complex128Array( 0 );
		} else {
			assert.ok( rfp, 'fixture ' + fx.name + ' has no rfp data (carry failed)' );
			// rfp is a flat real-imag pair sequence of length 2 * N*(N+1)/2.
			A = new Complex128Array( new Float64Array( rfp ).buffer );
		}
		WORK = new Float64Array( Math.max( N, 1 ) );

		result = zlanhf( args.norm, args.transr, args.uplo, N, A, 1, 0, WORK, 1, 0 );
		assertClose( result, fx.result, 1e-12, fx.name );
	});
});
