'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlangt = require( './dlangt.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlangt, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlangt;
