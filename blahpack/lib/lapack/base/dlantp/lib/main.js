
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlantp = require( './dlantp.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlantp, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlantp;
