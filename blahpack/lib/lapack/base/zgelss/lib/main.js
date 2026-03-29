'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgelss = require( './zgelss.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgelss, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgelss;
