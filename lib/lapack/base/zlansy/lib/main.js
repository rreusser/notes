'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlansy = require( './zlansy.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlansy, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlansy;
