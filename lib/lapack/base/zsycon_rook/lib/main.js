'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsyconRook = require( './zsycon_rook.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsyconRook, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsyconRook;
