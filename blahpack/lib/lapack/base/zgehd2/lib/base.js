/* eslint-disable max-len, max-params */
'use strict';
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlarf = require( '../../zlarf/lib/base.js' );
var zlarfg = require( '../../zlarfg/lib/base.js' );
var SCRATCH_TAU = new Complex128Array( 1 );
var SCRATCH_TAUv = reinterpret( SCRATCH_TAU, 0 );
var SCRATCH_ALPHA = new Complex128Array( 1 );
var SCRATCH_ALPHAv = reinterpret( SCRATCH_ALPHA, 0 );
function zgehd2( N, ilo, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK ) {
	var xStart, oAlpha, tauv, oTau, av, sa1, sa2, oA, i;
	av = reinterpret( A, 0 );
	sa1 = strideA1 * 2; sa2 = strideA2 * 2; oA = offsetA * 2;
	tauv = reinterpret( TAU, 0 );
	for ( i = ilo - 1; i < ihi - 1; i++ ) {
		oAlpha = oA + ( i + 1 ) * sa1 + i * sa2;
		SCRATCH_ALPHAv[ 0 ] = av[ oAlpha ]; SCRATCH_ALPHAv[ 1 ] = av[ oAlpha + 1 ];
		xStart = Math.min( i + 2, N - 1 );
		console.log('i='+i+' before zlarfg: alpha=(' + SCRATCH_ALPHAv[0] + ',' + SCRATCH_ALPHAv[1] + ')');
		zlarfg( ihi - i - 1, SCRATCH_ALPHA, 0, A, strideA1, offsetA + xStart * strideA1 + i * strideA2, SCRATCH_TAU, 0 );
		console.log('i='+i+' after zlarfg: tau=(' + SCRATCH_TAUv[0] + ',' + SCRATCH_TAUv[1] + '), alpha=(' + SCRATCH_ALPHAv[0] + ',' + SCRATCH_ALPHAv[1] + ')');
		av[ oAlpha ] = 1.0; av[ oAlpha + 1 ] = 0.0;
		console.log('i='+i+' before R: A[10]=' + av[10]);
		zlarf( 'R', ihi, ihi - i - 1, A, strideA1, offsetA + ( i + 1 ) * strideA1 + i * strideA2, SCRATCH_TAU, 0, A, strideA1, strideA2, offsetA + ( i + 1 ) * strideA2, WORK, strideWORK, offsetWORK );
		console.log('i='+i+' after R: A[10]=' + av[10] + ', tau=(' + SCRATCH_TAUv[0] + ',' + SCRATCH_TAUv[1] + ')');
		SCRATCH_TAUv[ 1 ] = -SCRATCH_TAUv[ 1 ];
		console.log('i='+i+' before L: conj(tau)=(' + SCRATCH_TAUv[0] + ',' + SCRATCH_TAUv[1] + ')');
		zlarf( 'L', ihi - i - 1, N - i - 1, A, strideA1, offsetA + ( i + 1 ) * strideA1 + i * strideA2, SCRATCH_TAU, 0, A, strideA1, strideA2, offsetA + ( i + 1 ) * strideA1 + ( i + 1 ) * strideA2, WORK, strideWORK, offsetWORK );
		console.log('i='+i+' after L: A[10]=' + av[10]);
		av[ oAlpha ] = SCRATCH_ALPHAv[ 0 ]; av[ oAlpha + 1 ] = SCRATCH_ALPHAv[ 1 ];
		oTau = ( offsetTAU + i * strideTAU ) * 2;
		tauv[ oTau ] = SCRATCH_TAUv[ 0 ]; tauv[ oTau + 1 ] = -SCRATCH_TAUv[ 1 ];
	}
	return 0;
}
module.exports = zgehd2;
