'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlascl2 = require( './dlascl2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlascl2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlascl2;
