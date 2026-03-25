

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlags2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlags2.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}


// DATA //

// Test parameter sets: [name, upper, a1, a2, a3, b1, b2, b3]
var cases = [
	[ 'upper_basic', true, 4.0, 2.0, 3.0, 1.0, 0.5, 2.0 ],
	[ 'lower_basic', false, 4.0, 2.0, 3.0, 1.0, 0.5, 2.0 ],
	[ 'upper_diagonal_b', true, 5.0, 3.0, 2.0, 1.0, 0.0, 1.0 ],
	[ 'lower_diagonal_b', false, 5.0, 3.0, 2.0, 1.0, 0.0, 1.0 ],
	[ 'upper_small_offdiag', true, 10.0, 1e-10, 5.0, 3.0, 1e-10, 2.0 ],
	[ 'lower_negative', false, -3.0, 4.0, -2.0, 1.0, -0.5, 3.0 ],
	[ 'upper_identity', true, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0 ],
	[ 'lower_large', false, 1e10, 5e9, 2e10, 3e10, 1e10, 4e10 ]
];


// TESTS //

cases.forEach( function each( c ) {
	test( 'dlags2: ' + c[ 0 ], function t() {
		var tc = findCase( c[ 0 ] );
		var out = dlags2( c[ 1 ], c[ 2 ], c[ 3 ], c[ 4 ], c[ 5 ], c[ 6 ], c[ 7 ] );
		assertClose( out.csu, tc.csu, 1e-14, 'csu' );
		assertClose( out.snu, tc.snu, 1e-14, 'snu' );
		assertClose( out.csv, tc.csv, 1e-14, 'csv' );
		assertClose( out.snv, tc.snv, 1e-14, 'snv' );
		assertClose( out.csq, tc.csq, 1e-14, 'csq' );
		assertClose( out.snq, tc.snq, 1e-14, 'snq' );
	});
});
