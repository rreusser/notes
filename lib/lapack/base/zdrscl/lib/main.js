'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zdrscl = require( './zdrscl.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zdrscl, 'ndarray', ndarray );


// EXPORTS //

module.exports = zdrscl;
