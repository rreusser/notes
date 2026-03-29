'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhecon = require( './zhecon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhecon, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhecon;
