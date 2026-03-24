

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dtrsyl = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtrsyl.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'dtrsyl: NN basic 2x2', function t() {
	var tc = findCase( 'NN basic 2x2' );
	var LDA = 4;
	var A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0; A[ 0 + 1*LDA ] = 2.0;
	A[ 1 + 1*LDA ] = 3.0;
	var B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 2.0; B[ 0 + 1*LDA ] = 1.0;
	B[ 1 + 1*LDA ] = 4.0;
	var C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 5.0; C[ 0 + 1*LDA ] = 6.0;
	C[ 1 + 0*LDA ] = 7.0; C[ 1 + 1*LDA ] = 8.0;
	var scale = new Float64Array( 1 );

	var info = dtrsyl( 'N', 'N', 1, 2, 2, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: NN isgn=-1', function t() {
	var tc = findCase( 'NN isgn=-1' );
	var LDA = 4;
	var A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0; A[ 0 + 1*LDA ] = 2.0;
	A[ 1 + 1*LDA ] = 3.0;
	var B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 2.0; B[ 0 + 1*LDA ] = 1.0;
	B[ 1 + 1*LDA ] = 4.0;
	var C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 5.0; C[ 0 + 1*LDA ] = 6.0;
	C[ 1 + 0*LDA ] = 7.0; C[ 1 + 1*LDA ] = 8.0;
	var scale = new Float64Array( 1 );

	var info = dtrsyl( 'N', 'N', -1, 2, 2, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: TN basic', function t() {
	var tc = findCase( 'TN basic' );
	var LDA = 4;
	var A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0; A[ 0 + 1*LDA ] = 2.0;
	A[ 1 + 1*LDA ] = 3.0;
	var B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 2.0; B[ 0 + 1*LDA ] = 1.0;
	B[ 1 + 1*LDA ] = 4.0;
	var C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 5.0; C[ 0 + 1*LDA ] = 6.0;
	C[ 1 + 0*LDA ] = 7.0; C[ 1 + 1*LDA ] = 8.0;
	var scale = new Float64Array( 1 );

	var info = dtrsyl( 'T', 'N', 1, 2, 2, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: TT basic', function t() {
	var tc = findCase( 'TT basic' );
	var LDA = 4;
	var A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0; A[ 0 + 1*LDA ] = 2.0;
	A[ 1 + 1*LDA ] = 3.0;
	var B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 2.0; B[ 0 + 1*LDA ] = 1.0;
	B[ 1 + 1*LDA ] = 4.0;
	var C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 5.0; C[ 0 + 1*LDA ] = 6.0;
	C[ 1 + 0*LDA ] = 7.0; C[ 1 + 1*LDA ] = 8.0;
	var scale = new Float64Array( 1 );

	var info = dtrsyl( 'T', 'T', 1, 2, 2, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: NT basic', function t() {
	var tc = findCase( 'NT basic' );
	var LDA = 4;
	var A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0; A[ 0 + 1*LDA ] = 2.0;
	A[ 1 + 1*LDA ] = 3.0;
	var B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 2.0; B[ 0 + 1*LDA ] = 1.0;
	B[ 1 + 1*LDA ] = 4.0;
	var C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 5.0; C[ 0 + 1*LDA ] = 6.0;
	C[ 1 + 0*LDA ] = 7.0; C[ 1 + 1*LDA ] = 8.0;
	var scale = new Float64Array( 1 );

	var info = dtrsyl( 'N', 'T', 1, 2, 2, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: M=0', function t() {
	var tc = findCase( 'M=0' );
	var A = new Float64Array( 16 );
	var B = new Float64Array( 16 );
	var C = new Float64Array( 16 );
	var scale = new Float64Array( 1 );

	var info = dtrsyl( 'N', 'N', 1, 0, 2, A, 1, 4, 0, B, 1, 4, 0, C, 1, 4, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
});

test( 'dtrsyl: N=0', function t() {
	var tc = findCase( 'N=0' );
	var A = new Float64Array( 16 );
	var B = new Float64Array( 16 );
	var C = new Float64Array( 16 );
	var scale = new Float64Array( 1 );

	var info = dtrsyl( 'N', 'N', 1, 2, 0, A, 1, 4, 0, B, 1, 4, 0, C, 1, 4, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
});

test( 'dtrsyl: NN 3x3 quasi-tri', function t() {
	var tc = findCase( 'NN 3x3 quasi-tri' );
	var LDA = 4;
	var A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0; A[ 0 + 1*LDA ] = 0.5; A[ 0 + 2*LDA ] = 0.3;
	A[ 1 + 1*LDA ] = 2.0; A[ 1 + 2*LDA ] = 0.4;
	A[ 2 + 1*LDA ] = -0.5; A[ 2 + 2*LDA ] = 2.0;
	var B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 3.0; B[ 0 + 1*LDA ] = 0.2; B[ 0 + 2*LDA ] = 0.1;
	B[ 1 + 1*LDA ] = 4.0; B[ 1 + 2*LDA ] = 0.3;
	B[ 2 + 1*LDA ] = -0.3; B[ 2 + 2*LDA ] = 4.0;
	var C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 1.0; C[ 0 + 1*LDA ] = 2.0; C[ 0 + 2*LDA ] = 3.0;
	C[ 1 + 0*LDA ] = 4.0; C[ 1 + 1*LDA ] = 5.0; C[ 1 + 2*LDA ] = 6.0;
	C[ 2 + 0*LDA ] = 7.0; C[ 2 + 1*LDA ] = 8.0; C[ 2 + 2*LDA ] = 9.0;
	var scale = new Float64Array( 1 );

	var info = dtrsyl( 'N', 'N', 1, 3, 3, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: M=1 N=1', function t() {
	var tc = findCase( 'M=1 N=1' );
	var LDA = 4;
	var A = new Float64Array( LDA * LDA );
	A[ 0 ] = 2.0;
	var B = new Float64Array( LDA * LDA );
	B[ 0 ] = 3.0;
	var C = new Float64Array( LDA * LDA );
	C[ 0 ] = 10.0;
	var scale = new Float64Array( 1 );

	var info = dtrsyl( 'N', 'N', 1, 1, 1, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: TN 3x3 quasi-tri', function t() {
	var tc = findCase( 'TN 3x3 quasi-tri' );
	var LDA = 4;
	var A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0; A[ 0 + 1*LDA ] = 0.5; A[ 0 + 2*LDA ] = 0.3;
	A[ 1 + 1*LDA ] = 2.0; A[ 1 + 2*LDA ] = 0.4;
	A[ 2 + 1*LDA ] = -0.5; A[ 2 + 2*LDA ] = 2.0;
	var B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 3.0; B[ 0 + 1*LDA ] = 0.2; B[ 0 + 2*LDA ] = 0.1;
	B[ 1 + 1*LDA ] = 4.0; B[ 1 + 2*LDA ] = 0.3;
	B[ 2 + 1*LDA ] = -0.3; B[ 2 + 2*LDA ] = 4.0;
	var C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 1.0; C[ 0 + 1*LDA ] = 2.0; C[ 0 + 2*LDA ] = 3.0;
	C[ 1 + 0*LDA ] = 4.0; C[ 1 + 1*LDA ] = 5.0; C[ 1 + 2*LDA ] = 6.0;
	C[ 2 + 0*LDA ] = 7.0; C[ 2 + 1*LDA ] = 8.0; C[ 2 + 2*LDA ] = 9.0;
	var scale = new Float64Array( 1 );

	var info = dtrsyl( 'T', 'N', 1, 3, 3, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: TT 3x3 quasi-tri', function t() {
	var tc = findCase( 'TT 3x3 quasi-tri' );
	var LDA = 4;
	var A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0; A[ 0 + 1*LDA ] = 0.5; A[ 0 + 2*LDA ] = 0.3;
	A[ 1 + 1*LDA ] = 2.0; A[ 1 + 2*LDA ] = 0.4;
	A[ 2 + 1*LDA ] = -0.5; A[ 2 + 2*LDA ] = 2.0;
	var B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 3.0; B[ 0 + 1*LDA ] = 0.2; B[ 0 + 2*LDA ] = 0.1;
	B[ 1 + 1*LDA ] = 4.0; B[ 1 + 2*LDA ] = 0.3;
	B[ 2 + 1*LDA ] = -0.3; B[ 2 + 2*LDA ] = 4.0;
	var C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 1.0; C[ 0 + 1*LDA ] = 2.0; C[ 0 + 2*LDA ] = 3.0;
	C[ 1 + 0*LDA ] = 4.0; C[ 1 + 1*LDA ] = 5.0; C[ 1 + 2*LDA ] = 6.0;
	C[ 2 + 0*LDA ] = 7.0; C[ 2 + 1*LDA ] = 8.0; C[ 2 + 2*LDA ] = 9.0;
	var scale = new Float64Array( 1 );

	var info = dtrsyl( 'T', 'T', 1, 3, 3, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: NT 3x3 quasi-tri', function t() {
	var tc = findCase( 'NT 3x3 quasi-tri' );
	var LDA = 4;
	var A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0; A[ 0 + 1*LDA ] = 0.5; A[ 0 + 2*LDA ] = 0.3;
	A[ 1 + 1*LDA ] = 2.0; A[ 1 + 2*LDA ] = 0.4;
	A[ 2 + 1*LDA ] = -0.5; A[ 2 + 2*LDA ] = 2.0;
	var B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 3.0; B[ 0 + 1*LDA ] = 0.2; B[ 0 + 2*LDA ] = 0.1;
	B[ 1 + 1*LDA ] = 4.0; B[ 1 + 2*LDA ] = 0.3;
	B[ 2 + 1*LDA ] = -0.3; B[ 2 + 2*LDA ] = 4.0;
	var C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 1.0; C[ 0 + 1*LDA ] = 2.0; C[ 0 + 2*LDA ] = 3.0;
	C[ 1 + 0*LDA ] = 4.0; C[ 1 + 1*LDA ] = 5.0; C[ 1 + 2*LDA ] = 6.0;
	C[ 2 + 0*LDA ] = 7.0; C[ 2 + 1*LDA ] = 8.0; C[ 2 + 2*LDA ] = 9.0;
	var scale = new Float64Array( 1 );

	var info = dtrsyl( 'N', 'T', 1, 3, 3, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});
