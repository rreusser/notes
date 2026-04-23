'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dpbcon = require( './dpbcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpbcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpbcon;
