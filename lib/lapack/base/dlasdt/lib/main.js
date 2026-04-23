'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlasdt = require( './dlasdt.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasdt, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasdt;
