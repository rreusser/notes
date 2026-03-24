'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var ztrsyl = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ztrsyl.jsonl' ), 'utf8' ).trim().split( '\n' );
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
	assert.equal( actual.length, expected.length, msg + ': length mismatch (' + actual.length + ' vs ' + expected.length + ')' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Build the standard 2x2 test matrices A, B, C for the complex Sylvester tests.
* Returns { A, B, C } as Complex128Arrays with Float64Array views.
* Uses LDA=MAXN=3 in Fortran, but JS uses compact N=2 (stride = 1, N).
*/
function build2x2() {
	var N = 2;
	var A = new Complex128Array( N * N );
	var Av = reinterpret( A, 0 );
	// A(0,0) = (1, 0.5)
	Av[ (0 + 0*N)*2 ] = 1.0; Av[ (0 + 0*N)*2 + 1 ] = 0.5;
	// A(0,1) = (0.3, 0.1)
	Av[ (0 + 1*N)*2 ] = 0.3; Av[ (0 + 1*N)*2 + 1 ] = 0.1;
	// A(1,1) = (3, -0.5)
	Av[ (1 + 1*N)*2 ] = 3.0; Av[ (1 + 1*N)*2 + 1 ] = -0.5;

	var B = new Complex128Array( N * N );
	var Bv = reinterpret( B, 0 );
	// B(0,0) = (2, 0.3)
	Bv[ (0 + 0*N)*2 ] = 2.0; Bv[ (0 + 0*N)*2 + 1 ] = 0.3;
	// B(0,1) = (0.4, -0.2)
	Bv[ (0 + 1*N)*2 ] = 0.4; Bv[ (0 + 1*N)*2 + 1 ] = -0.2;
	// B(1,1) = (4, 0.7)
	Bv[ (1 + 1*N)*2 ] = 4.0; Bv[ (1 + 1*N)*2 + 1 ] = 0.7;

	var C = new Complex128Array( N * N );
	var Cv = reinterpret( C, 0 );
	// C(0,0) = (5, 1)
	Cv[ (0 + 0*N)*2 ] = 5.0; Cv[ (0 + 0*N)*2 + 1 ] = 1.0;
	// C(0,1) = (6, -1)
	Cv[ (0 + 1*N)*2 ] = 6.0; Cv[ (0 + 1*N)*2 + 1 ] = -1.0;
	// C(1,0) = (7, 2)
	Cv[ (1 + 0*N)*2 ] = 7.0; Cv[ (1 + 0*N)*2 + 1 ] = 2.0;
	// C(1,1) = (8, -2)
	Cv[ (1 + 1*N)*2 ] = 8.0; Cv[ (1 + 1*N)*2 + 1 ] = -2.0;

	return { A: A, B: B, C: C, Cv: Cv, N: N };
}

/**
* Build the 3x3 test matrices for larger tests.
*/
function build3x3() {
	var N = 3;
	var A = new Complex128Array( N * N );
	var Av = reinterpret( A, 0 );
	Av[ (0 + 0*N)*2 ] = 1.0; Av[ (0 + 0*N)*2 + 1 ] = 0.5;
	Av[ (0 + 1*N)*2 ] = 0.3; Av[ (0 + 1*N)*2 + 1 ] = 0.1;
	Av[ (0 + 2*N)*2 ] = 0.1; Av[ (0 + 2*N)*2 + 1 ] = -0.05;
	Av[ (1 + 1*N)*2 ] = 2.0; Av[ (1 + 1*N)*2 + 1 ] = -0.3;
	Av[ (1 + 2*N)*2 ] = 0.2; Av[ (1 + 2*N)*2 + 1 ] = 0.15;
	Av[ (2 + 2*N)*2 ] = 3.0; Av[ (2 + 2*N)*2 + 1 ] = 0.8;

	var B = new Complex128Array( N * N );
	var Bv = reinterpret( B, 0 );
	Bv[ (0 + 0*N)*2 ] = 4.0; Bv[ (0 + 0*N)*2 + 1 ] = -0.2;
	Bv[ (0 + 1*N)*2 ] = 0.5; Bv[ (0 + 1*N)*2 + 1 ] = 0.3;
	Bv[ (0 + 2*N)*2 ] = 0.2; Bv[ (0 + 2*N)*2 + 1 ] = -0.1;
	Bv[ (1 + 1*N)*2 ] = 5.0; Bv[ (1 + 1*N)*2 + 1 ] = 0.4;
	Bv[ (1 + 2*N)*2 ] = 0.3; Bv[ (1 + 2*N)*2 + 1 ] = 0.2;
	Bv[ (2 + 2*N)*2 ] = 6.0; Bv[ (2 + 2*N)*2 + 1 ] = -0.6;

	var C = new Complex128Array( N * N );
	var Cv = reinterpret( C, 0 );
	Cv[ (0 + 0*N)*2 ] = 1.0; Cv[ (0 + 0*N)*2 + 1 ] = 2.0;
	Cv[ (0 + 1*N)*2 ] = 3.0; Cv[ (0 + 1*N)*2 + 1 ] = -1.0;
	Cv[ (0 + 2*N)*2 ] = 5.0; Cv[ (0 + 2*N)*2 + 1 ] = 0.5;
	Cv[ (1 + 0*N)*2 ] = 2.0; Cv[ (1 + 0*N)*2 + 1 ] = 4.0;
	Cv[ (1 + 1*N)*2 ] = 4.0; Cv[ (1 + 1*N)*2 + 1 ] = -2.0;
	Cv[ (1 + 2*N)*2 ] = 6.0; Cv[ (1 + 2*N)*2 + 1 ] = 1.0;
	Cv[ (2 + 0*N)*2 ] = 3.0; Cv[ (2 + 0*N)*2 + 1 ] = 6.0;
	Cv[ (2 + 1*N)*2 ] = 5.0; Cv[ (2 + 1*N)*2 + 1 ] = -3.0;
	Cv[ (2 + 2*N)*2 ] = 7.0; Cv[ (2 + 2*N)*2 + 1 ] = 1.5;

	return { A: A, B: B, C: C, Cv: Cv, N: N };
}


// TESTS //

test( 'ztrsyl: NN basic 2x2', function t() {
	var tc = findCase( 'NN basic 2x2' );
	var m = build2x2();
	var scale = new Float64Array( 1 );

	var info = ztrsyl( 'no-transpose', 'no-transpose', 1, m.N, m.N, m.A, 1, m.N, 0, m.B, 1, m.N, 0, m.C, 1, m.N, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( m.Cv ), tc.C, 1e-12, 'C' );
});

test( 'ztrsyl: NN isgn=-1', function t() {
	var tc = findCase( 'NN isgn=-1' );
	var m = build2x2();
	var scale = new Float64Array( 1 );

	var info = ztrsyl( 'no-transpose', 'no-transpose', -1, m.N, m.N, m.A, 1, m.N, 0, m.B, 1, m.N, 0, m.C, 1, m.N, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( m.Cv ), tc.C, 1e-12, 'C' );
});

test( 'ztrsyl: CN basic (conjugate transpose A)', function t() {
	var tc = findCase( 'CN basic' );
	var m = build2x2();
	var scale = new Float64Array( 1 );

	var info = ztrsyl( 'conjugate-transpose', 'no-transpose', 1, m.N, m.N, m.A, 1, m.N, 0, m.B, 1, m.N, 0, m.C, 1, m.N, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( m.Cv ), tc.C, 1e-12, 'C' );
});

test( 'ztrsyl: NC basic (conjugate transpose B)', function t() {
	var tc = findCase( 'NC basic' );
	var m = build2x2();
	var scale = new Float64Array( 1 );

	var info = ztrsyl( 'no-transpose', 'conjugate-transpose', 1, m.N, m.N, m.A, 1, m.N, 0, m.B, 1, m.N, 0, m.C, 1, m.N, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( m.Cv ), tc.C, 1e-12, 'C' );
});

test( 'ztrsyl: CC basic (conjugate transpose both)', function t() {
	var tc = findCase( 'CC basic' );
	var m = build2x2();
	var scale = new Float64Array( 1 );

	var info = ztrsyl( 'conjugate-transpose', 'conjugate-transpose', 1, m.N, m.N, m.A, 1, m.N, 0, m.B, 1, m.N, 0, m.C, 1, m.N, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( m.Cv ), tc.C, 1e-12, 'C' );
});

test( 'ztrsyl: M=0 quick return', function t() {
	var tc = findCase( 'M=0' );
	var A = new Complex128Array( 4 );
	var B = new Complex128Array( 4 );
	var C = new Complex128Array( 4 );
	var scale = new Float64Array( 1 );

	var info = ztrsyl( 'no-transpose', 'no-transpose', 1, 0, 2, A, 1, 2, 0, B, 1, 2, 0, C, 1, 2, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
});

test( 'ztrsyl: N=0 quick return', function t() {
	var tc = findCase( 'N=0' );
	var A = new Complex128Array( 4 );
	var B = new Complex128Array( 4 );
	var C = new Complex128Array( 4 );
	var scale = new Float64Array( 1 );

	var info = ztrsyl( 'no-transpose', 'no-transpose', 1, 2, 0, A, 1, 2, 0, B, 1, 2, 0, C, 1, 2, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
});

test( 'ztrsyl: M=1 N=1', function t() {
	var tc = findCase( 'M=1 N=1' );
	var A = new Complex128Array( 1 );
	var B = new Complex128Array( 1 );
	var C = new Complex128Array( 1 );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var Cv = reinterpret( C, 0 );
	Av[ 0 ] = 2.0; Av[ 1 ] = 1.0;
	Bv[ 0 ] = 3.0; Bv[ 1 ] = -1.0;
	Cv[ 0 ] = 10.0; Cv[ 1 ] = 5.0;
	var scale = new Float64Array( 1 );

	var info = ztrsyl( 'no-transpose', 'no-transpose', 1, 1, 1, A, 1, 1, 0, B, 1, 1, 0, C, 1, 1, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( Cv ), tc.C, 1e-12, 'C' );
});

test( 'ztrsyl: NN 3x3', function t() {
	var tc = findCase( 'NN 3x3' );
	var m = build3x3();
	var scale = new Float64Array( 1 );

	var info = ztrsyl( 'no-transpose', 'no-transpose', 1, m.N, m.N, m.A, 1, m.N, 0, m.B, 1, m.N, 0, m.C, 1, m.N, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( m.Cv ), tc.C, 1e-12, 'C' );
});

test( 'ztrsyl: CN 3x3', function t() {
	var tc = findCase( 'CN 3x3' );
	var m = build3x3();
	var scale = new Float64Array( 1 );

	var info = ztrsyl( 'conjugate-transpose', 'no-transpose', 1, m.N, m.N, m.A, 1, m.N, 0, m.B, 1, m.N, 0, m.C, 1, m.N, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( m.Cv ), tc.C, 1e-12, 'C' );
});

test( 'ztrsyl: CC isgn=-1 3x3', function t() {
	var tc = findCase( 'CC isgn=-1 3x3' );
	var m = build3x3();
	var scale = new Float64Array( 1 );

	var info = ztrsyl( 'conjugate-transpose', 'conjugate-transpose', -1, m.N, m.N, m.A, 1, m.N, 0, m.B, 1, m.N, 0, m.C, 1, m.N, 0, scale );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( m.Cv ), tc.C, 1e-12, 'C' );
});
