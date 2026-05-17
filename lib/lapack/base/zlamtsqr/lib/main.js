
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlamtsqr = require( './zlamtsqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlamtsqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlamtsqr;
