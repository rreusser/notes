'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztrcon = require( './ztrcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztrcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztrcon;
