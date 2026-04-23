'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zheconRook = require( './zhecon_rook.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zheconRook, 'ndarray', ndarray );


// EXPORTS //

module.exports = zheconRook;
