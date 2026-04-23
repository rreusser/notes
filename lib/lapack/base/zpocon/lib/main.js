'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zpocon = require( './zpocon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpocon, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpocon;
