'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarra = require( './dlarra.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarra, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarra;
