
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztbcon = require( './ztbcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztbcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztbcon;
