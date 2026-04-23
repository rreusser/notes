
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsytf2Rook = require( './zsytf2_rook.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsytf2Rook, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsytf2Rook;
