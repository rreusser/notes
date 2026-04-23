
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlacrm = require( './zlacrm.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlacrm, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlacrm;
