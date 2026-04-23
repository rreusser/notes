
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtpcon = require( './dtpcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtpcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtpcon;
