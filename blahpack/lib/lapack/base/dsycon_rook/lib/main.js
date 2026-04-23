
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsyconRook = require( './dsycon_rook.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsyconRook, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsyconRook;
