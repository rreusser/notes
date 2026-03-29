'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztrttf = require( './ztrttf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztrttf, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztrttf;
