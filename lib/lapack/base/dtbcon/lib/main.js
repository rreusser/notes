
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtbcon = require( './dtbcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtbcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtbcon;
