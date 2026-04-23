
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlat2c = require( './zlat2c.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlat2c, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlat2c;
