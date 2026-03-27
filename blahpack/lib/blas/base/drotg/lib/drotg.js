

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Constructs a Givens plane rotation.
*
* @param {Float64Array} ab - ab
* @param {integer} strideAB - strideAB
* @param {Float64Array} cs - cs
* @param {integer} strideCS - strideCS
* @returns {*} result
*/
function drotg( ab, strideAB, cs, strideCS ) {
	var oab;
	var ocs;

	oab = stride2offset( N, strideAB );
	ocs = stride2offset( N, strideCS );
	return base( ab, strideAB, oab, cs, strideCS, ocs );
}


// EXPORTS //

module.exports = drotg;
