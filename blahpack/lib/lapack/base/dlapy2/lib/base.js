'use strict';

// MAIN //

/**
* Returns sqrt(x**2 + y**2), taking care not to cause unnecessary
* overflow and unnecessary underflow.
*
* @private
* @param {number} x - first value
* @param {number} y - second value
* @returns {number} sqrt(x**2 + y**2)
*/
function dlapy2( x, y ) {
	var xabs;
	var yabs;
	var w;
	var z;

	// Handle NaN
	if ( x !== x ) {
		return x;
	}
	if ( y !== y ) {
		return y;
	}

	xabs = Math.abs( x );
	yabs = Math.abs( y );
	w = Math.max( xabs, yabs );
	z = Math.min( xabs, yabs );

	if ( z === 0.0 || w > 1.7976931348623157e+308 ) {
		return w;
	}
	return w * Math.sqrt( 1.0 + ( z / w ) * ( z / w ) );
}


// EXPORTS //

module.exports = dlapy2;
