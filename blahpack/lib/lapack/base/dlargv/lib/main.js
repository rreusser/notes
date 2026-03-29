'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlargv = require( './dlargv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlargv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlargv;
