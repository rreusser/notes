
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlansp = require( './zlansp.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlansp, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlansp;
