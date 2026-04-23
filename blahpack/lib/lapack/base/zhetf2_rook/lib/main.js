
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhetf2Rook = require( './zhetf2_rook.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhetf2Rook, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhetf2Rook;
