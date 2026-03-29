'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtrcon = require( './dtrcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrcon;
