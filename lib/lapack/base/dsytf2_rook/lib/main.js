
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsytf2Rook = require( './dsytf2_rook.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsytf2Rook, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsytf2Rook;
