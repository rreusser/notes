

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {*} v - v
* @param {*} idx - idx
* @returns {Object} aggressive-early-deflation result `{ ns, nd }`
*/
function zlaqr2( v, idx ) {
	return base( v, idx );
}


// EXPORTS //

module.exports = zlaqr2;
