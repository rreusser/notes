'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlangt = require( './zlangt.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlangt, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlangt;
