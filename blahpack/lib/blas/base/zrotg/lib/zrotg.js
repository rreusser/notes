

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Returns `re*re + im*im` without complex abs overhead.
*
* @param {number} re - re
* @param {number} im - im
* @returns {*} result
*/
function zrotg( re, im ) {
	return base( re, im );
}


// EXPORTS //

module.exports = zrotg;
