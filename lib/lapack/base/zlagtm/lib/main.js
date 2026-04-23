'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlagtm = require( './zlagtm.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlagtm, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlagtm;
