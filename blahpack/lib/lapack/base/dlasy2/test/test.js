'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlasy2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlasy2.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'dlasy2: n1=1 n2=1 basic', function t() {
	var tc = findCase( 'n1=1 n2=1 basic' );
	var TL = new Float64Array([ 3.0, 0, 0, 0 ]);
	var TR = new Float64Array([ 2.0, 0, 0, 0 ]);
	var B = new Float64Array([ 10.0, 0, 0, 0 ]);
	var X = new Float64Array( 4 );
	var scale = new Float64Array( 1 );
	var xnorm = new Float64Array( 1 );

	var info = dlasy2( false, false, 1, 1, 1, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( Array.from( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: n1=1 n2=2', function t() {
	var tc = findCase( 'n1=1 n2=2' );
	var TL = new Float64Array([ 2.0, 0, 0, 0 ]);
	var TR = new Float64Array([ 1.0, -0.5, 0.5, 1.0 ]);
	var B = new Float64Array([ 3.0, 0, 4.0, 0 ]); // col-major 1x2: B(1,1)=3, B(1,2)=4
	var X = new Float64Array( 4 );
	var scale = new Float64Array( 1 );
	var xnorm = new Float64Array( 1 );

	var info = dlasy2( false, false, 1, 1, 2, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( Array.from( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: n1=2 n2=1', function t() {
	var tc = findCase( 'n1=2 n2=1' );
	var TL = new Float64Array([ 2.0, -0.5, 0.5, 2.0 ]);
	var TR = new Float64Array([ 1.0, 0, 0, 0 ]);
	var B = new Float64Array([ 3.0, 4.0, 0, 0 ]); // col-major 2x1: B(1,1)=3, B(2,1)=4
	var X = new Float64Array( 4 );
	var scale = new Float64Array( 1 );
	var xnorm = new Float64Array( 1 );

	var info = dlasy2( false, false, 1, 2, 1, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( Array.from( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: n1=2 n2=2', function t() {
	var tc = findCase( 'n1=2 n2=2' );
	var TL = new Float64Array([ 1.0, -0.5, 0.5, 1.0 ]);
	var TR = new Float64Array([ 2.0, -0.3, 0.3, 2.0 ]);
	var B = new Float64Array([ 1.0, 3.0, 2.0, 4.0 ]); // col-major: B(1,1)=1, B(2,1)=3, B(1,2)=2, B(2,2)=4
	var X = new Float64Array( 4 );
	var scale = new Float64Array( 1 );
	var xnorm = new Float64Array( 1 );

	var info = dlasy2( false, false, 1, 2, 2, TL, 1, 2, 0, TR, 1, 2, 0, B, 1, 2, 0, scale, X, 1, 2, 0, xnorm );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( xnorm[ 0 ], tc.xnorm, 1e-14, 'xnorm' );
	assertArrayClose( Array.from( X ), tc.X, 1e-14, 'X' );
});

test( 'dlasy2: quick return for n1=0', function t() {
	var X = new Float64Array( 4 );
	var scale = new Float64Array( 1 );
	var xnorm = new Float64Array( 1 );
	var info = dlasy2( false, false, 1, 0, 1, new Float64Array(4), 1, 2, 0, new Float64Array(4), 1, 2, 0, new Float64Array(4), 1, 2, 0, scale, X, 1, 2, 0, xnorm );
	assert.strictEqual( info, 0 );
});
