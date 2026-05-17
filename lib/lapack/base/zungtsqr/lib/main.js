
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zungtsqr = require( './zungtsqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zungtsqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zungtsqr;
