'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlanht = require( './zlanht.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlanht, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlanht;
