
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlantp = require( './zlantp.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlantp, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlantp;
