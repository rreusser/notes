'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgttrs = require( './dgttrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgttrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgttrs;
